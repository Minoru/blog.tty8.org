---
title: Патч Advanced Copy для coreutils 8.5
published: 2010-12-23T13:10:00Z
categories: 
tags: linux
description: Добавляем прогресс-бар в cp.
---

Привет!

Полтора часа назад в [псто](http://psto.net/) пролетело упоминание об
интересном патче для `coreutils` [Advanced
Copy](http://www.beatex.org/web/advancedcopy.html), добавляющем в `cp` и `mv`
прогресс-бар. Выглядит это так:

```
% cp -g ~/torrents/downloads/Patent_Absurdity_HQ_768kbit.ogv /dev/null
cp: переписать «/dev/null»? y344

0 files copied so far...                                 135,7 MiB / 159,5 MiB
[||||||||||||||||||||||||||||||||||||||||||||||||||||||||||-----------] 85,1 %
Copying at   1,0 GiB/s (about 0h 0m 0s remaining)
...ru/torrents/downloads/Patent_Absurdity_HQ_768kbit.ogv 135,7 MiB / 159,4 MiB
[||||||||||||||||||||||||||||||||||||||||||||||||||||||||||-----------] 85,2 %
```

Польза этой штуки лично для меня под вопросом, но я решил всё же попробовать
её. Так как в Debian Squeeze сейчас уже `coreutils` версии 8.5, патч пришлось
немного поправить. Модифицированную версию можно [скачать](http://ix.io/1kI)
или [смотреть онлайн](http://ix.io/1kI/diff) (вторая ссылка отличается наличием
подсветки). Также на всякий случай добавлю его в пост — как говаривал
**virens**, сторонние сервисы могут отказывать, но если уж упадёт блог…

```diff
diff -Nru coreutils-8.5/src/copy.c coreutils-8.5-1/src/copy.c
--- coreutils-8.5/src/copy.c	2010-04-20 22:52:04.000000000 +0300
+++ coreutils-8.5-1/src/copy.c	2010-12-23 13:51:23.000000000 +0200
@@ -457,6 +457,56 @@
   return lchmod (name, mode);
 }
 
+/* BEGIN progress mod */
+static void file_progress_bar ( char * _cDest, int _iBarLength, int _iProgress, int _iTotal )
+{
+  // write number to progress bar
+  float fPercent = ( float ) _iProgress / ( float ) _iTotal * 100.f;
+  sprintf ( _cDest + ( _iBarLength - 6 ), "%4.1f", fPercent );
+  // remove zero
+  _cDest[_iBarLength - 2] = ' ';
+
+  // fill rest with '-'
+  int i;
+  for ( i = 1; i <= _iBarLength - 9; i++ )
+  {
+    if ( fPercent > ( float ) ( i - 1 ) / ( _iBarLength - 10 ) * 100.f )
+      _cDest[i] = '|';
+    else
+      _cDest[i] = '-';
+  }
+}
+
+int file_size_format ( char * _cDst, int _iSize, int _iCounter )
+{
+  int iCounter = _iCounter;
+  double dSize = ( double ) _iSize;
+  while ( dSize >= 1000. )
+  {
+    dSize /= 1024.;
+    iCounter++;
+  }
+
+  /* get unit */
+  char * sUnit;
+  if ( iCounter == 0 )
+    sUnit = "B";
+  else if ( iCounter == 1 )
+    sUnit = "KiB";
+  else if ( iCounter == 2 )
+    sUnit = "MiB";
+  else if ( iCounter == 3 )
+    sUnit = "GiB";
+  else if ( iCounter == 4 )
+    sUnit = "TiB";
+  else
+    sUnit = "N/A";
+
+  /* write number */
+  return sprintf ( _cDst, "%5.1f %s", dSize, sUnit );
+}
+/* END progress mod */
+
 /* Copy a regular file from SRC_NAME to DST_NAME.
    If the source file contains holes, copies holes and blocks of zeros
    in the source file as holes in the destination file.
@@ -709,8 +759,146 @@
       buf_alloc = xmalloc (buf_size + buf_alignment_slop);
       buf = ptr_align (buf_alloc, buf_alignment);
 
+      /* BEGIN progress mod */
+      /* create a field of 6 lines */
+      char ** cProgressField = ( char ** ) calloc ( 6, sizeof ( char * ) );
+      /* get console width */
+      int iBarLength = 80;
+      struct winsize win;
+      if ( ioctl (STDOUT_FILENO, TIOCGWINSZ, (char *) &win) == 0 && win.ws_col > 0 )
+         iBarLength = win.ws_col;
+      /* create rows */
+      int it;
+      for ( it = 0; it < 6; it++ )
+      {
+        cProgressField[it] = ( char * ) malloc ( iBarLength + 1 );
+        /* init with spaces */
+        int j;
+        for ( j = 0; j < iBarLength; j++ )
+          cProgressField[it][j] = ' ';
+        cProgressField[it][iBarLength] = '\0';
+      }
+
+      /* global progress bar? */
+      if ( g_iTotalSize )
+      {
+        /* init global progress bar */
+        cProgressField[2][0] = '[';
+        cProgressField[2][iBarLength - 8] = ']';
+        cProgressField[2][iBarLength - 7] = ' ';
+        cProgressField[2][iBarLength - 1] = '%';
+
+        /* total size */
+        cProgressField[1][iBarLength - 11] = '/';
+        file_size_format ( cProgressField[1] + iBarLength - 9, g_iTotalSize, 1 );
+
+        /* show how many files were written */
+        int sum_length = sprintf ( cProgressField[1], "%d files copied so far...", g_iFilesCopied );
+        cProgressField[1][sum_length] = ' ';
+      }
+
+      /* truncate filename? */
+      int fn_length;
+      if ( strlen ( src_name ) > iBarLength - 22 )
+        fn_length =
+          sprintf ( cProgressField[4], "...%s", src_name + ( strlen ( src_name ) - iBarLength + 25 ) );
+      else
+        fn_length = sprintf ( cProgressField[4], "%s", src_name );
+      cProgressField[4][fn_length] = ' ';
+
+      /* filesize */
+      cProgressField[4][iBarLength - 11] = '/';
+      file_size_format ( cProgressField[4] + iBarLength - 9, src_open_sb.st_size, 0 );
+
+      int iCountDown = 1;
+      char * sProgressBar = cProgressField[5];
+      sProgressBar[0] = '[';
+      sProgressBar[iBarLength - 8] = ']';
+      sProgressBar[iBarLength - 7] = ' ';
+      sProgressBar[iBarLength - 1] = '%';
+
+      /* this will always save the time in between */
+      struct timeval last_time;
+      gettimeofday ( & last_time, NULL );
+      int last_size = g_iTotalWritten;
+      /* END progress mod */
+
       for (;;)
         {
+          if (progress) {
+          /* BEGIN progress mod */
+          /* update countdown */
+          iCountDown--;
+          if ( iCountDown < 0 )
+            iCountDown = 100;
+
+          /* just print one line with the percentage, but not always */
+          if ( iCountDown == 0 )
+          {
+            /* calculate current speed */
+            struct timeval cur_time;
+            gettimeofday ( & cur_time, NULL );
+            int cur_size = g_iTotalWritten + n_read_total / 1024;
+            int usec_elapsed = cur_time.tv_usec - last_time.tv_usec;
+            double sec_elapsed = ( double ) usec_elapsed / 1000000.f;
+            sec_elapsed += ( double ) ( cur_time.tv_sec - last_time.tv_sec );
+            int copy_speed = ( int ) ( ( double ) ( cur_size - last_size )
+              / sec_elapsed );
+            char s_copy_speed[20];
+            file_size_format ( s_copy_speed, copy_speed, 1 );
+            /* update vars */
+            last_time = cur_time;
+            last_size = cur_size;
+
+            /* how many time has passed since the start? */
+            int isec_elapsed = cur_time.tv_sec - g_oStartTime.tv_sec;
+            int sec_remaining = ( int ) ( ( double ) isec_elapsed / cur_size
+              * g_iTotalSize ) - isec_elapsed;
+            int min_remaining = sec_remaining / 60;
+            sec_remaining -= min_remaining * 60;
+            int hours_remaining = min_remaining / 60;
+            min_remaining -= hours_remaining * 60;
+            /* print out */
+            sprintf ( cProgressField[3],
+              "Copying at %s/s (about %dh %dm %ds remaining)", s_copy_speed,
+              hours_remaining, min_remaining, sec_remaining );
+
+            int fs_len;
+            if ( g_iTotalSize )
+            {
+              /* global progress bar */
+              file_progress_bar ( cProgressField[2], iBarLength,
+                                  g_iTotalWritten + n_read_total / 1024, g_iTotalSize );
+
+              /* print the global status */
+              fs_len = file_size_format ( cProgressField[1] + iBarLength - 21,
+                                              g_iTotalWritten + n_read_total / 1024, 1 );
+              cProgressField[1][iBarLength - 21 + fs_len] = ' ';
+            }
+
+            /* current progress bar */
+            file_progress_bar ( sProgressBar, iBarLength, n_read_total, src_open_sb.st_size );
+
+            /* print the status */
+            fs_len = file_size_format ( cProgressField[4] + iBarLength - 21, n_read_total, 0 );
+            cProgressField[4][iBarLength - 21 + fs_len] = ' ';
+
+            /* print the field */
+            for ( it = g_iTotalSize ? 0 : 3; it < 6; it++ )
+            {
+              printf ( "\033[K%s\n", cProgressField[it] );
+              if ( strlen ( cProgressField[it] ) < iBarLength )
+                printf ( "" );
+            }
+            if ( g_iTotalSize )
+              printf ( "\r\033[6A" );
+            else
+              printf ( "\r\033[3A" );
+            fflush ( stdout );
+          }
+          /* END progress mod */
+          }
+
           word *wp = NULL;
 
           ssize_t n_read = read (source_desc, buf, buf_size);
@@ -791,6 +979,19 @@
                  /proc with linux kernels from at least 2.6.9 .. 2.6.29.  */
             }
         }
+if (progress) {
+      /* BEGIN progress mod */
+      /* update total size */
+      g_iTotalWritten += n_read_total / 1024;
+      g_iFilesCopied++;
+
+      int i;
+      for ( i = 0; i < 6; i++ )
+        free ( cProgressField[i] );
+      free ( cProgressField );
+      /* END progress mod */
+}
+
 
       /* If the file ends with a `hole', we need to do something to record
          the length of the file.  On modern systems, calling ftruncate does
diff -Nru coreutils-8.5/src/copy.h coreutils-8.5-1/src/copy.h
--- coreutils-8.5/src/copy.h	2010-04-20 22:52:04.000000000 +0300
+++ coreutils-8.5-1/src/copy.h	2010-12-23 13:52:40.000000000 +0200
@@ -223,6 +223,10 @@
      Create destination directories as usual. */
   bool symbolic_link;
 
+  /* if true, draw a nice progress bar on screen */
+  bool progress_bar;
+
+
   /* If true, do not copy a nondirectory that has an existing destination
      with the same or newer modification time. */
   bool update;
@@ -281,4 +285,15 @@
 bool chown_failure_ok (struct cp_options const *);
 mode_t cached_umask (void);
 
+/* BEGIN progress mod */
+int file_size_format ( char * _cDst, int _iSize, int _iCounter );
+
+long g_iTotalSize;
+long g_iTotalWritten;
+int g_iFilesCopied;
+struct timeval g_oStartTime;
+int g_iTotalFiles;
+bool progress;
+/* END progress mod */
+
 #endif
diff -Nru coreutils-8.5/src/cp.c coreutils-8.5-1/src/cp.c
--- coreutils-8.5/src/cp.c	2010-03-13 17:14:09.000000000 +0200
+++ coreutils-8.5-1/src/cp.c	2010-12-23 13:55:44.000000000 +0200
@@ -139,6 +139,7 @@
   {"target-directory", required_argument, NULL, 't'},
   {"update", no_argument, NULL, 'u'},
   {"verbose", no_argument, NULL, 'v'},
+  {"progress-bar", no_argument, NULL, 'g'},
   {GETOPT_HELP_OPTION_DECL},
   {GETOPT_VERSION_OPTION_DECL},
   {NULL, 0, NULL, 0}
@@ -176,6 +177,7 @@
   -f, --force                  if an existing destination file cannot be\n\
                                  opened, remove it and try again (redundant if\n\
                                  the -n option is used)\n\
+  -g, --progress-bar           add progress-bar\n\
   -i, --interactive            prompt before overwrite (overrides a previous -n\n\
                                   option)\n\
   -H                           follow command-line symbolic links in SOURCE\n\
@@ -612,6 +614,57 @@
                quote (file[n_files - 1]));
     }
 
+    struct timeval start_time;
+if (progress) {
+    /* BEGIN progress mod */
+    g_iTotalSize = 0;
+    g_iFilesCopied = 0;
+    g_iTotalWritten = 0;
+
+    /* save time */
+    gettimeofday ( & start_time, NULL );
+    g_oStartTime = start_time;
+
+    printf ( "Calculating total size... \r" );
+    fflush ( stdout );
+    long iTotalSize = 0;
+    int iFiles = n_files;
+    if ( ! target_directory )
+      iFiles = n_files - 1;
+    int j;
+    for (j = 0; j < iFiles; j++)
+    {
+      /* call du -s for each file */
+      /* create command */
+      char command[1024];
+      sprintf ( command, "du -s \"%s\"", file[j] );
+      /* TODO: replace all quote signs in file[i] */
+
+      FILE *fp;
+      char output[1024];
+
+      /* run command */
+      fp = popen(command, "r");
+      if (fp == NULL || fgets(output, sizeof(output)-1, fp) == NULL) {
+        printf("failed to run du.\n" );
+      }
+      else
+      {
+        /* isolate size */
+        strchr ( output, '\t' )[0] = '\0';
+        iTotalSize += atol ( output );
+
+        printf ( "Calculating total size... %ld\r", iTotalSize );
+        fflush ( stdout );
+      }
+
+      /* close */
+      pclose(fp);
+    }
+    g_iTotalSize = iTotalSize;
+    /* END progress mod */
+}
+
   if (target_directory)
     {
       /* cp file1...filen edir
@@ -754,6 +807,46 @@
       ok = copy (source, new_dest, 0, x, &unused, NULL);
     }
 
+if (progress) {
+    /* BEGIN progress mod */
+    /* remove everything */
+    int i;
+    if ( g_iTotalSize )
+    {
+      for ( i = 0; i < 6; i++ )
+        printf ( "\033[K\n" );
+      printf ( "\r\033[6A" );
+    }
+    else
+    {
+      for ( i = 0; i < 3; i++ )
+        printf ( "\033[K\n" );
+      printf ( "\r\033[3A" );
+    }
+
+    /* save time */
+    struct timeval end_time;
+    gettimeofday ( & end_time, NULL );
+    int usec_elapsed = end_time.tv_usec - start_time.tv_usec;
+    double sec_elapsed = ( double ) usec_elapsed / 1000000.f;
+    sec_elapsed += ( double ) ( end_time.tv_sec - start_time.tv_sec );
+
+    /* get total size */
+    char sTotalWritten[20];
+    file_size_format ( sTotalWritten, g_iTotalSize, 1 );
+    /* TODO: using g_iTotalWritten would be more correct, but is less accurate */
+
+    /* calculate speed */
+    int copy_speed = ( int ) ( ( double ) g_iTotalWritten / sec_elapsed );
+    char s_copy_speed[20];
+    file_size_format ( s_copy_speed, copy_speed, 1 );
+
+    /* good-bye message */
+    printf ( "%d files (%s) copied in %.1f seconds (%s/s).\n", g_iFilesCopied, sTotalWritten,
+             sec_elapsed, s_copy_speed );
+    /* END progress mod */
+}
+
   return ok;
 }
 
@@ -785,6 +878,7 @@
   x->recursive = false;
   x->sparse_mode = SPARSE_AUTO;
   x->symbolic_link = false;
+  x->progress_bar = false;
   x->set_mode = false;
   x->mode = 0;
 
@@ -923,7 +1017,7 @@
      we'll actually use backup_suffix_string.  */
   backup_suffix_string = getenv ("SIMPLE_BACKUP_SUFFIX");
 
-  while ((c = getopt_long (argc, argv, "abdfHilLnprst:uvxPRS:T",
+  while ((c = getopt_long (argc, argv, "abdfgHilLnprst:uvxPRS:T",
                            long_opts, NULL))
          != -1)
     {
@@ -975,6 +1069,10 @@
           x.unlink_dest_after_failed_open = true;
           break;
 
+        case 'g':
+          progress = true;
+          break;
+
         case 'H':
           x.dereference = DEREF_COMMAND_LINE_ARGUMENTS;
           break;
diff -Nru coreutils-8.5/src/mv.c coreutils-8.5-1/src/mv.c
--- coreutils-8.5/src/mv.c	2010-01-01 15:06:47.000000000 +0200
+++ coreutils-8.5-1/src/mv.c	2010-12-23 13:59:21.000000000 +0200
@@ -64,6 +64,7 @@
   {"target-directory", required_argument, NULL, 't'},
   {"update", no_argument, NULL, 'u'},
   {"verbose", no_argument, NULL, 'v'},
+  {"progress-bar", no_argument, NULL, 'g'},
   {GETOPT_HELP_OPTION_DECL},
   {GETOPT_VERSION_OPTION_DECL},
   {NULL, 0, NULL, 0}
@@ -159,10 +160,94 @@
 static bool
 do_move (const char *source, const char *dest, const struct cp_options *x)
 {
+  struct timeval start_time;
+
+  if(progress) {
+    /* BEGIN progress mod */
+    g_iTotalSize = 0;
+    g_iFilesCopied = 0;
+    g_iTotalWritten = 0;
+
+    gettimeofday (& start_time, NULL);
+    g_oStartTime = start_time;
+
+    printf ("Calculating total size... \r");
+    fflush (stdout);
+    long iTotalSize = 0;
+    /* call du -s for each file */
+    /* create command */
+    char command[1024];
+    sprintf ( command, "du -s \"%s\"", source );
+    /* TODO: replace all quote signs in file[i] */
+
+    FILE *fp;
+    char output[1024];
+
+    /* run command */
+    fp = popen(command, "r");
+    if (fp == NULL || fgets(output, sizeof(output)-1, fp) == NULL) {
+      printf("failed to run du.\n" );
+    }
+    else
+    {
+      /* isolate size */
+      strchr ( output, '\t' )[0] = '\0';
+      iTotalSize += atol ( output );
+      printf ( "Calculating total size... %ld\r", iTotalSize );
+      fflush ( stdout );
+    }
+
+    /* close */
+    pclose(fp);
+    g_iTotalSize = iTotalSize;
+    /* END progress mod */
+
+  }
+
   bool copy_into_self;
   bool rename_succeeded;
   bool ok = copy (source, dest, false, x, &copy_into_self, &rename_succeeded);
 
+  if (progress) {
+    /* BEGIN progress mod */
+    /* remove everything */
+    int i;
+    if ( g_iTotalSize )
+    {
+      for ( i = 0; i < 6; i++ )
+        printf ( "\033[K\n" );
+      printf ( "\r\033[6A" );
+    }
+    else
+    {
+      for ( i = 0; i < 3; i++ )
+        printf ( "\033[K\n" );
+      printf ( "\r\033[3A" );
+    }
+
+    /* save time */
+    struct timeval end_time;
+    gettimeofday ( & end_time, NULL );
+    int usec_elapsed = end_time.tv_usec - start_time.tv_usec;
+    double sec_elapsed = ( double ) usec_elapsed / 1000000.f;
+    sec_elapsed += ( double ) ( end_time.tv_sec - start_time.tv_sec );
+
+    /* get total size */
+    char sTotalWritten[20];
+    file_size_format ( sTotalWritten, g_iTotalSize, 1 );
+    /* TODO: using g_iTotalWritten would be more correct, but is less accurate */
+
+    /* calculate speed */
+    int copy_speed = ( int ) ( ( double ) g_iTotalWritten / sec_elapsed );
+    char s_copy_speed[20];
+    file_size_format ( s_copy_speed, copy_speed, 1 );
+
+    /* good-bye message */
+    printf ( "%d files (%s) moved in %.1f seconds (%s/s).\n", g_iFilesCopied, sTotalWritten,
+             sec_elapsed, s_copy_speed );
+    /* END progress mod */
+  }
+
   if (ok)
     {
       char const *dir_to_remove;
@@ -298,6 +383,7 @@
       --backup[=CONTROL]       make a backup of each existing destination file\n\
   -b                           like --backup but does not accept an argument\n\
   -f, --force                  do not prompt before overwriting\n\
+  -g, --progress-bar	       add progress-bar\n\
   -i, --interactive            prompt before overwrite\n\
   -n, --no-clobber             do not overwrite an existing file\n\
 If you specify more than one of -i, -f, -n, only the final one takes effect.\n\
@@ -366,7 +452,7 @@
      we'll actually use backup_suffix_string.  */
   backup_suffix_string = getenv ("SIMPLE_BACKUP_SUFFIX");
 
-  while ((c = getopt_long (argc, argv, "bfint:uvS:T", long_options, NULL))
+  while ((c = getopt_long (argc, argv, "bfint:uvgS:T", long_options, NULL))
          != -1)
     {
       switch (c)
@@ -411,6 +497,9 @@
         case 'v':
           x.verbose = true;
           break;
+	    case 'g':
+          progress = true;
+	      break;
         case 'S':
           make_backups = true;
           backup_suffix_string = optarg;
```

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-12-23T17:02:19.234+02:00, butch wrote:</p>
<p class='hakyll-convert-comment-body'>
почему бы просто не использовать rsync --progress если так хочется прогрессбар?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-12-24T01:51:55.348+02:00, virens wrote:</p>
<p class='hakyll-convert-comment-body'>
Если прогресс-бар будет опцией, то почему бы и нет. А то копируешь чего-нибудь,
а оно сидит себе и ничего не говорит.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-12-24T07:37:09.496+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>2 butch</b>:
Тот факт, что тебе нужен прогрессбар, как правило осознаёшь только когда
копирование уже запущено :) Да и переучиваться набирать вместо cp гоаздо более
длинное rsync тупо лень. Хотя не исключено, что это лучше, чем указанный патч.
Я себе пакетик собрал, поюзаю месяцок — погляжу, насколько мне эти прогрессбары
нужны.

<b>2 virens</b>:
Ты только учти, что патч этот вроде как в апстрим не принимают, что вполне
логично — coreutils и так уже довольно жирные. То есть если хочешь попробовать,
берёшь патч, мой перевод статьи про пересборку пакетов, закрываешься в тёмной
комнате (хотя можно и без этого) и собираешь себе свои собственные coreutils.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-12-24T10:33:43.631+02:00, muhas wrote:</p>
<p class='hakyll-convert-comment-body'>
ы. а я как-раз пишу заметку о сем деле, а тут и твой пост в рсс подоспевает ^_^
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-01-19T01:43:25.010+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
пользуюсь iotop когда надо оценить время завершения процесса копирования (или
другой дисковой примитивной операции)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T16:56:59.641+02:00, Альгис wrote:</p>
<p class='hakyll-convert-comment-body'>
Раньше приходилось собирать пакеты из исходников, но вот патчить их не
приходилось пока. Хочется научиться. Порылся в гугле, кой-чего нашел, но патч
накладывается с ошибками. У меня Debian Sqeeze, сoreutils_8.5-1. Может я чего
делаю не так? Вот мой краткий &quot;лог&quot;:

```
mkdir coreutils-drc
cd coreutils-src
apt-get source coreutils
wget http://ix.io/1kI # тот самый патч
patch -p1 &lt; 1kI
patching file src/copy.c
Hunk #1 FAILED at 457.
Hunk #2 FAILED at 709.
Hunk #3 FAILED at 791.
3 out of 3 hunks FAILED -- saving rejects to file src/copy.c.rej
patching file src/copy.h
Hunk #1 FAILED at 223.
Hunk #2 FAILED at 281.
2 out of 2 hunks FAILED -- saving rejects to file src/copy.h.rej
patching file src/cp.c
Hunk #1 FAILED at 139.
Hunk #2 FAILED at 176.
Hunk #3 FAILED at 612.
Hunk #4 FAILED at 754.
Hunk #5 FAILED at 785.
Hunk #6 FAILED at 923.
Hunk #7 FAILED at 975.
7 out of 7 hunks FAILED -- saving rejects to file src/cp.c.rej
patching file src/mv.c
Hunk #1 FAILED at 64.
Hunk #2 FAILED at 159.
Hunk #3 FAILED at 298.
Hunk #4 FAILED at 366.
Hunk #5 FAILED at 411.
5 out of 5 hunks FAILED -- saving rejects to file src/mv.c.rej
```
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T19:16:24.721+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>2 Анонимный:</b>
iotop, конечно, штука хорошая, но прогрессбар всяко лучше. Впрочем, спустя два
месяца использования Advanced Copy заявляю: оно не нужно. Все равно в нужный
момент забываешь указать нужную опцию, а  если добавить алиас, то оно скорее
мешет, чем помогает. Так что лучше уж никак, чем так.

<b>2 Альгис</b>:
Судя по всему, накладывается больше одного патча и некоторые из них
пересекаются (т.е. первый меняет строку, а когда дело доходит до второго, он
видит несоответствие шаблону и ругается). [Юзай
dpatch!](/posts/2010-12-23-how-to-use-dpatch.html)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T21:19:21.999+02:00, Альгис wrote:</p>
<p class='hakyll-convert-comment-body'>
Чет нихрена не получается.... Может выложишь пакет для публики? :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T21:32:33.734+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Увы, пакет с сорцами я не создавал, так что остались только .deb'ы под i386:

*(Здесь были мёртвые ссылки на файлы, которых у меня больше нет, поэтому текст
я тоже удалил).*
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T22:32:41.293+02:00, Альгис wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо и на том! А зачем mktemp? Там что-то важное?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-02-27T22:40:42.924+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Да нет, просто он собирается вместе с coreutils из того же набора исходников.
</p>
</div>




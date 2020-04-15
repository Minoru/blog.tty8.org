---
title: Nested structs as signal arguments in Qt
language: english
description: I defined a struct within a class, and used it in one of its slots.
    Connections with that slot failed at runtime, claiming I need to register
    the type—even though I already did. The problem turned out to be
    a (documented) limitation in Moc. The workaround is—spoiler alert!—to use
    fully-qualified type names in signals and slots.
tags: programming, qt
---

[Worker objects][worker-objects] in Qt are used to offload lengthy operations
onto separate threads, out of the main event loop. They usually operate on data
that already exists, e.g. building a palette from a `QBitmap`, or counting words
in a `QTextEdit`.

However, there also exists another class of usages, where the worker object is
tasked with *producing* some data, basically out of the ether. This happened to
me just today: I wanted to pre-process a video to calculate its duration,
resolution, and also average and maximum bit-rates. The most natural way to
return that info would be to emit it inside a signal, so I wrote:

```c++
#include <QObject>
#include <QSize>
#include <chrono>

class Worker : public QObject
{
    Q_OBJECT

    QString m_inputFilepath;

public:
    struct Result {
        QSize resolution;
        std::chrono::milliseconds duration {0};
        double avgBitrateMbps = -1.0;
        double maxBitrateMbps = -1.0;
    };

    explicit Worker(QObject *parent = nullptr) {
        qRegisterMetaType<Result>();
    }

    void setInputFilepath(QString filepath);

public slots:
    void process();

signals:
    void finished(Result);
};

Q_DECLARE_METATYPE(Worker::Result)
```

This compiled without a warning—and I'm pretty thorough with those, enabling
`-Wall` and `-Wextra`, and turning them into errors with `-Werror`. So I went on
to use it:

```c++
auto thread = new QThread();
auto worker = new Worker();
worker->setInputFilepath(m_videoFilepath);
worker->moveToThread(thread);
connect(thread, &QThread::started, worker, &Worker::process);
connect(worker, &Worker::finished, this, &MainWindow::storeVideoInfo);
connect(worker, &Worker::finished, thread, &QThread::quit);
connect(worker, &Worker::finished, worker, &QObject::deleteLater);
connect(thread, &QThread::finished, thread, &QObject::deleteLater);
thread->start();
```

Again, it compiled—no warnings, no errors. But when I ran the program…

```
QObject::connect: Cannot queue arguments of type 'Result'
(Make sure 'Result' is registered using qRegisterMetaType().)
QObject::connect: Cannot queue arguments of type 'Result'
(Make sure 'Result' is registered using qRegisterMetaType().)
```

Uh-oh! What's going on? Clearly I *did* register my type, it's right there in
`Worker`'s constructor…

<hr />

As is usual with thorniest of Qt problems, this one is related to Moc—the
program that implements signals and slots. It processes headers with `Q_OBJECT`
in them, and generates additional files (like moc\_worker.cpp) that implement
Qt's internals.

This particular issue is [a limitation in Moc][moc-fqn]. `QObject::connect`
compares types as strings, and it's completely unaware of scopes. In my listings
above, it doesn't understand that `Result` in `void finished(Result)` and the
`Worker::Result` throughout the rest of the code is one and the same type.

To fix this, fully qualify the type in signal's definition:

```c++
void finished(Worker::Result);
```

[worker-objects]: https://wiki.qt.io/QThreads_general_usage
    "QThreads general usage — Qt Wiki"

[moc-fqn]: https://doc.qt.io/qt-5/moc.html#enums-and-typedefs-must-be-fully-qualified-for-signal-and-slot-parameters
    "Using the Meta-Object Compiler (moc) — Qt 5 Docs"

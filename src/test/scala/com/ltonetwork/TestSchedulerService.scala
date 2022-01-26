package com.ltonetwork

import monix.execution.{Cancelable, ExecutionModel}
import monix.execution.schedulers.{SchedulerService, TestScheduler}

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration}

case class TestSchedulerService(scheduler: TestScheduler, time: TestTime) extends SchedulerService {
  private var isRunning = true
  override def isShutdown: Boolean = !isRunning
  override def isTerminated: Boolean = !isRunning

  override def shutdown(): Unit = { isRunning = false }
  override def awaitTermination(timeout: Long, unit: TimeUnit, awaitOn: ExecutionContext): Future[Boolean] = Future.successful(true)

  override def withExecutionModel(em: ExecutionModel): SchedulerService = copy(scheduler = scheduler.withExecutionModel(em))

  override def execute(command: Runnable): Unit = scheduler.execute(command)
  override def reportFailure(t: Throwable): Unit = scheduler.reportFailure(t)

  override def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable): Cancelable = scheduler.scheduleOnce(initialDelay, unit, r)
  override def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable): Cancelable = scheduler.scheduleWithFixedDelay(initialDelay, delay, unit, r)
  override def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable): Cancelable = scheduler.scheduleAtFixedRate(initialDelay, period, unit, r)

  override def clockRealTime(unit: TimeUnit): Long = scheduler.clockRealTime(unit)
  override def clockMonotonic(unit: TimeUnit): Long = scheduler.clockMonotonic(unit)

  override def executionModel: ExecutionModel = scheduler.executionModel

  def tickOne(): Boolean = scheduler.tickOne()
  def tick(d: FiniteDuration): Unit = {
    time.advance(d)
    scheduler.tick(d)
  }
}

object TestSchedulerService {
  def apply(): TestSchedulerService = new TestSchedulerService(TestScheduler(), new TestTime())
  def apply(time: TestTime): TestSchedulerService = new TestSchedulerService(TestScheduler(), time)
}

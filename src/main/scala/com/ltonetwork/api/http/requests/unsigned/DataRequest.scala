package com.ltonetwork.api.http.requests.unsigned

import com.ltonetwork.state.DataEntry

case class DataRequest(version: Byte, sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

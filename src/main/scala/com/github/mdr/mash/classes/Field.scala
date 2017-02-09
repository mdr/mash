package com.github.mdr.mash.classes

import com.github.mdr.mash.inference.Type

case class Field(name: String,
                 summaryOpt: Option[String] = None,
                 fieldType: Type,
                 descriptionOpt: Option[String] = None)
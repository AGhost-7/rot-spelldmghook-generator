
package object pipeOps {
	implicit class pipeOpForAnything[A](val a: A) extends AnyVal {
		def |> [B](fun: A => B): B = fun(a)
	}
}


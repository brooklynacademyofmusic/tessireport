withr::local_package("checkmate")
withr::local_package("mockery")


# send_email --------------------------------------------------------------

test_that("send_email complains if subject, emails, or smtp are not set correctly", {
  sendmail <- mock()
  stub(send_email, "sendmail", sendmail)

  expect_error(send_email(),"subject.+missing")

  stub(send_email, "config::get", mock(NULL,
                                       "test@test.com", NULL,
                                       "test@test.com", list(host.name = "blah")))

  expect_error(send_email("subject", "body"),"email.+sender.+recipients")
  expect_error(send_email("subject", "body"),"smtp server")
})

test_that("send_email passes on parameters to sendmail", {
  sendmail <- mock()
  stub(send_email, "sendmail", sendmail)
  stub(send_email, "config::get", mock("test@test.com", list(hostname = "blah")))

  expect_silent(send_email("subject","body"))
  expect_length(mock_args(sendmail),1)
  expect_match(mock_args(sendmail)[[1]]$subject,"subject")
  expect_match(mock_args(sendmail)[[1]]$msg$text,"body")
  expect_equal(mock_args(sendmail)[[1]]$from,"test@test.com")
  expect_equal(mock_args(sendmail)[[1]]$to,"test@test.com")
  expect_equal(mock_args(sendmail)[[1]]$control,list(smtpServer="blah",smtpPort=NULL))
})

test_that("send_email sends an email", {
  stub(send_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  mail <- send_email(engine="debug","This is a test email")
  expect_match(mail,"This is a test email",all = F)
  expect_match(mail,"test@test.com",all = F)
  expect_match(mail,"Content-Type: text/html",all = F)
})

test_that("send_email sends attachments", {
  stub(send_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  filename <- tempfile()
  writeLines("Hi!!!! 😊✨",filename)
  mail <- send_email(engine="debug","This is a test email",attachments = c(attachment.txt = filename))
  expect_match(mail,"This is a test email",all = F)
  expect_match(mail,"test@test.com",all = F)
  expect_match(mail,"Content-Type: text/html",all = F)
  expect_match(mail,gsub("=","",base64enc::base64encode(charToRaw("Hi!!!! 😊✨"))),all = F)
})


# send_xlsx ---------------------------------------------------------------

test_that("send_xlsx calls send_email with the specified args", {
  send_email <- mock()
  stub(send_xlsx, "send_email", send_email)

  a_table <- data.table(x = seq(1000))

  send_xlsx(a_table)
  expect_equal(mock_args(send_email)[[1]]$subject,paste("a_table",Sys.Date()))
  expect_equal(mock_args(send_email)[[1]]$body,paste("Sent by",Sys.info()["nodename"]))
  expect_equal(mock_args(send_email)[[1]]$emails,"ssyzygy@bam.org")
  expect_match(names(mock_args(send_email)[[1]]$attachments),paste0("a_table_",Sys.Date(),".xlsx"))


  send_xlsx(a_table, subject = "subject", body = "body", emails = "me@me.com",
            basename = "b_table")
  expect_equal(mock_args(send_email)[[2]]$subject,"subject")
  expect_equal(mock_args(send_email)[[2]]$body,"body")
  expect_equal(mock_args(send_email)[[2]]$emails,"me@me.com")
  expect_match(names(mock_args(send_email)[[2]]$attachments),paste0("b_table_",Sys.Date(),".xlsx"))
})

//W432J0E1 JOB (640W4320100W432J0E1,W100),'RTN W432E1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM EXEC W016RECV                                                            
&VCOM                                                                           
W432.*.W43218 FB 299                                                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W432J0E1                                         

//W412J0E4 JOB (640W4120100W412J0E4,W100),'RTN W412E4',                         
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
W412.*.W41222 VB 136                                                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W412J0E4                                         

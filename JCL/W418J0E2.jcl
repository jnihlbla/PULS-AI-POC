//W418J0E2 JOB (640W4180100W418J0E2,W100),'RTN W418E2',                         
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
W418.*.W41802 VB 33                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W418J0E2                                         

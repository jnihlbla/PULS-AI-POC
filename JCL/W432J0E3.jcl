//W432J0E3 JOB (640W4320100W432J0E3,W100),'RTN W432E3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.CUSTOMERLIST                                           
¤DSOUT W432.W432X1*Market*.W43218                                               
¤RECFM FB                                                                       
¤LRECL 299                                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W432J0E3                                         

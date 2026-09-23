//W463J0E6 JOB (640W4630100W463J0E6,W100),'RTN W463E6',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.DIRECTBUSINESSINVOICE                                  
¤DSOUT W463.W463X6MQ.W46370                                                     
¤RECFM FB                                                                       
¤LRECL 204                                                                      
/*                                                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J0E6                                         

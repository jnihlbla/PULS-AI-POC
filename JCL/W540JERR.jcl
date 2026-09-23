//W540J010 JOB (650W5400100W540J010,W100),'RTN W540V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W540    EXEC W540P010                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W540J010                                         

//W231J072 JOB (650W2310100W231J072,W100),'RTN W231S2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P072                                                         
//W23172.W23172D4 DD *                                                          
&IDUSER.&URVAL.&DC                                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J072                                         

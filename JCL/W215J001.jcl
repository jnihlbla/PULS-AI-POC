//W215J001 JOB (650W2150100W215J001,W100),'RTN W215S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W215    EXEC W215P001                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W215J001                                         

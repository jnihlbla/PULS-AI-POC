//W111J060 JOB (650W1110100W111J060,W100),'RTN W100V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W111    EXEC W111P060                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W111J060                                         

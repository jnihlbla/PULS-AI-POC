//W560J080 JOB (670W5600100W560J080,W100),'RTN W553V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W560    EXEC W560P080                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560J080                                         

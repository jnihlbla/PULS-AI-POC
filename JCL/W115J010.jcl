//W115J010 JOB (670W1150100W115J010,W100),'RTN W115V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//W115    EXEC W115P010                                                         
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W115J010                                         

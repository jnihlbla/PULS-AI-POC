//W612R1RS JOB (640W6120100W612R1RS,W100),'RTN W612R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W612R1                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612R1RS                                         

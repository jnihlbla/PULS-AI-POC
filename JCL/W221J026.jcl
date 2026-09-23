//W221J026 JOB (650W2210100W221J026,W100),'RTN W200V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P026                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W221J026                                         

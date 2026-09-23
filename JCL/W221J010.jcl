//W221J010 JOB (670W2210100W221J010,W100),'RTN W200D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P010                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W221J010                                         

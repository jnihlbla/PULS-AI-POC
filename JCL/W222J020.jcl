//W222J020 JOB (670W2220100W222J020,W100),'RTN W200V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W222    EXEC W222P020                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W222J020                                         

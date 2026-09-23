//W479J015 JOB (640W4790100W479J015,W100),'RTN W479V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W479    EXEC W479P015                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J015                                         

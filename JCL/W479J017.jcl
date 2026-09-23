//W479J017 JOB (640W4790100W479J017,W100),'RTN W479V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W479    EXEC W479P017                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J017                                         

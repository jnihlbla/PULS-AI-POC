//W479J013 JOB (640W4790100W479J013,W100),'RTN W479V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG1                                                               
//*                                                                             
//W479    EXEC W479P013,CPU=4                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J013                                         

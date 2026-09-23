//W555J005 JOB (650W5550100W555J005,W100),'RTN W555B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W555    EXEC W555P005                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W555J005                                         

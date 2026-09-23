//WL10J010 JOB (670WL100100WL10J010,W100),'RTN WL10S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTL                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//WL10    EXEC WL10P010                                                         
//*                                                                             
//WL1010.WL1010D1 DD *                                                          
&URVAL                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WL10J010                                         

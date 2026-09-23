//W416J001 JOB (670W4160100W416J001,W100),'RTN W416S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W416    EXEC W416P001                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W416J001                                         

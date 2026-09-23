//W479J020 JOB (670W4790100W479J020,W100),'RTN W479B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P020                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J020                                         

//W479J120 JOB (670W4790100W479J120,W100),'RTN W479D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W479    EXEC W479P020                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J120                                         

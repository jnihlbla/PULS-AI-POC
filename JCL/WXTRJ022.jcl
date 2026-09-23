//WXTRJ022 JOB (670WXTR0100WXTRJ022,W100),'RTN WXTRV2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W015    EXEC W015P033,                                                        
//             DSIN=WXTR.WXTRV2.WXTR22(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ022                                         

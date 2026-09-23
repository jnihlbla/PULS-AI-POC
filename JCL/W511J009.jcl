//W511J009 JOB (670W5110100W511J009,W100),'RTN W500V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0,LINES=9999                                        
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//WZ14    EXEC WZ14DAP2,DSIN=W511.W500V1.W51108(+0),CPU=2                       
//SYSIN        DD *                                                             
W51108-001                                                                      
A                                                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W511J009                                         

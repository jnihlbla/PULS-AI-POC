//W515J150 JOB (640W5100100W515J150,W100),'RTN W515M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W515.W515M2.W51550(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W515.W515M2.W51550(+0)                                    
//SYSIN           DD *                                                          
W51550-001                                                                      
W51550                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515J150                                         

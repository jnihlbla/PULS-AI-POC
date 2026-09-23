//W223D1MS JOB (640W2230100W223D1MS,W100),'RTN W223D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
/*ROUTE XEQ LOCAL                                                               
//***********************************************************                   
//* INFO  ARTIKLAR MED FELAKTIG URSPRUNGSKOD                                    
//* W223.W223D1.W22311(+0)                                                      
//***********************************************************                   
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W223.W223D1.W22311(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W223.W223D1.W22311(+0)                                    
//SYSIN           DD *                                                          
W223D1MS-001                                                                    
W223D1MS                                                                        
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W223D1MS                                         

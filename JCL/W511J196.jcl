//W511J196 JOB (640W5110100W511J196,W100),'RTN W500V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WUT.W500V1.W51196(+0)                                
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WUT.W500V1.W51196(+0)                                     
//SYSIN           DD *                                                          
W51196-001                                                                      
W51196                                                                          
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W511J196                                         

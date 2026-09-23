//WF10J072 JOB (640WF100100WF10J072,W100),'RTN WF10D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*******************************************************************           
//* MAIL OM BLOCK SAP UPDATES VIA D&P                                           
//* WF10.WF10D1.WF1072, PARMAID AND COUNTRY CODE                                
//*******************************************************************           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10D1.WF1072(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WF10.WF10D1.WF1072(+0)                                    
//SYSIN           DD *                                                          
WF1072-001                                                                      
WF1072                                                                          
//    ENDIF                                                                     
//EMPTY2 EXEC WEMPTST,DSIN=WF10.WF10D1.WF1005A(+0)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=WF10.WF10D1.WF1005A(+0)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J072                                         

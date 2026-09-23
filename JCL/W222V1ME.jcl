//W222V1ME JOB (640W2220100W222V1ME,W100),'RTN W222V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//***********************************************************                   
//*      ÖVERFÖRING LISTA LARM HÖG ELLER LÅG ORDERINGÅNG CDC*                   
//* =>   MEMO       TILL ANSKAFFARNA                        *                   
//***********************************************************                   
//*                                                                             
//*EMPTY1  EXEC WEMPTST,DSIN=W222.W222V1.W22241(+0)                             
//*                                                                             
//*                                                                             
//*    IF (EMPTY1.T.RC NE 4) THEN                                               
//*                                                                             
//*                                                                             
//*W222    EXEC WMEMOSND                                                        
//*M.APIFILE  DD DSN=W222.W222V1.W22241(+0),DISP=OLD                            
//*M.SEND     DD DUMMY                                                          
//*    ENDIF                                                                    
//*                                                                             
//*                                                                             
//***********************************************************                   
//*      ÖVERFÖRING LISTA LARM PROGNOSEN HAR BLIVIT NOLL    *                   
//* =>   MEMO       TILL ANSKAFFARNA                        *                   
//***********************************************************                   
//*                                                                             
//EMPTY2  EXEC WEMPTST,DSIN=W222.W222V1.W22246(+0)                              
//*                                                                             
//*                                                                             
//    IF (EMPTY2.T.RC NE 4) THEN                                                
//W222    EXEC WMEMOSND                                                         
//M.APIFILE  DD DSN=W222.W222V1.W22246(+0),DISP=OLD                             
//M.SEND     DD DUMMY                                                           
//    ENDIF                                                                     
//*                                                                             
//***********************************************************                   
//*      ÖVERFÖRING LISTA LARM HÖG ELLER LÅG ORDERINGÅNG DC *                   
//* =>   DISTRIBUTION & PRINT                               *                   
//***********************************************************                   
//*                                                                             
//*EMPTY3  EXEC WEMPTST,DSIN=W222.W222V1.W22241A(+0)                            
//*     IF (EMPTY3.T.RC NE 4) THEN                                              
//*                                                                             
//* EXEC WZ14PDAP,DSIN=W222.W222V1.W22241A(+0)                                  
//*SYSIN           DD *                                                         
//*W22241-001                                                                   
//*W22241                                                                       
//*     ENDIF                                                                   
//*                                                                             
//EMPTY4  EXEC WEMPTST,DSIN=W222.W222V1.W22246A(+0)                             
//    IF (EMPTY4.T.RC NE 4) THEN                                                
//*                                                                             
// EXEC WZ14PDAP,DSIN=W222.W222V1.W22246A(+0)                                   
//SYSIN           DD *                                                          
W22246-001                                                                      
W22246                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222V1ME                                         

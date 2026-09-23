//W500V1ME JOB (640W5100100W500V1ME,W100),'RTN W500V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//******************************************************************            
//* MEMO TILL EK-AVD OM VECKANS ANTAL VERIFIKAT TILL SAP DAG FÖR DAG            
//* W510.W500V1.W51078,DISP=SHR                                                 
//*******************************************************************           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W510.W500V1.W51078                                   
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W510.W500V1.W51078                                        
//SYSIN           DD *                                                          
W51078-001                                                                      
W51078                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W500V1ME                                         

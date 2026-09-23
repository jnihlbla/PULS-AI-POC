//W517J139 JOB (640W5170100W517J139,W100),'RTN W517V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*******************************************************************           
//* MAIL TILL EKONOMI/INVENTERING VIA D&P                                       
//* W517.W517V1.W51742, INVENTORYS ADJUSTMENT                                   
//*******************************************************************           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W517.W517V1.W51742(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W517.W517V1.W51742(+0)                                    
//SYSIN           DD *                                                          
W51739-001                                                                      
W51739                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W517J139                                         

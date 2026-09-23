//W513J014 JOB (670W5130100W513J014,W100),'RTN W513V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W513    EXEC W513P014                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W513.W513V1.W513141(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513141(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
MA                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W513.W513V1.W513142(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513142(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
CN                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W513.W513V1.W513143(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513143(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
PF                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W513.W513V1.W513144(+1)                              
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513144(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
AS                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY5 EXEC WEMPTST,DSIN=W513.W513V1.W513145(+1)                              
//    IF (EMPTY5.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513145(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
KR                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY6 EXEC WEMPTST,DSIN=W513.W513V1.W513146(+1)                              
//    IF (EMPTY6.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513146(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
AE                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY7 EXEC WEMPTST,DSIN=W513.W513V1.W513147(+1)                              
//    IF (EMPTY7.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513147(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
TR                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY8 EXEC WEMPTST,DSIN=W513.W513V1.W513148(+1)                              
//    IF (EMPTY8.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513148(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
MY                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY9 EXEC WEMPTST,DSIN=W513.W513V1.W513149(+1)                              
//    IF (EMPTY9.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W513149(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
TH                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY10 EXEC WEMPTST,DSIN=W513.W513V1.W51314A(+1)                             
//    IF (EMPTY10.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W513.W513V1.W51314A(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
TW                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY11 EXEC WEMPTST,DSIN=W513.W513V1.W51314B(+1)                             
//    IF (EMPTY11.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W513.W513V1.W51314B(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
BR                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY12 EXEC WEMPTST,DSIN=W513.W513V1.W51314C(+1)                             
//    IF (EMPTY12.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W513.W513V1.W51314C(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
MX                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY13 EXEC WEMPTST,DSIN=W513.W513V1.W51314D(+1)                             
//    IF (EMPTY13.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W513.W513V1.W51314D(+1)                                   
//SYSIN           DD *                                                          
MAN-WEEKLY-AREA                                                                 
ZA                                                                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J014                                         

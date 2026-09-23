//W221J127 JOB (650W2210100W221J127,W100),'RTN W200V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*JOBPARM LINES=5000                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*     Denna JCL saknar PROCEDUR-call.                                         
//*                                                                             
//*     Nedanstående fil är redan skapad i Jobb W221J027                        
//*                                                                             
//* ---- SÄNDER EXCEL-FILEN TILL BESTÄLLAREN ---                                
//* ----            Lotta Andrén             ---                                
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WUT.W200V1.W22153SP                                  
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WUT.W200V1.W22153SP                                       
//SYSIN           DD *                                                          
W221127-001                                                                     
W221127                                                                         
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W221J127                                         

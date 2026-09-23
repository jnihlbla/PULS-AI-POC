//W476Z3PS JOB (670W4760100W476Z3PS,W100),'RTN W476SP',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* PACKNINGS SPEC. TILL OLIKA TRANSP.                                          
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476SP.W4764P(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476SP.W4764P(+0),DISP=(OLD,DELETE)                         
//   ELSE                                                                       
//* PACKNINGS SPEC. TILL OLIKA TRANSP                                           
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W476.W476SP.W4764P(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VEDIUNB125.IFCSUM                                       
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z3PS                                         

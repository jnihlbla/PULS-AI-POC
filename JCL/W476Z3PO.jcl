//W476Z3PO JOB (670W4760100W476Z3PO,W100),'RTN W476SE',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* SKEPPN.UPPG. FÖR ÖVERFÖRING TILL OLIKA TRANSP. GENOM AMTRIX / POSTEN        
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476SE.W47641(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476SE.W47641(+0),DISP=(OLD,DELETE)                         
//   ELSE                                                                       
//* IFCSUM TILL EDI                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W476.W476SE.W47641(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VEDIUNB125.IFCSUM                                       
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z3PO                                         

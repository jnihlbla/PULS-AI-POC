//W571J008 JOB (640W5710100W571J008,W100),'RTN W571B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W571    EXEC W571P008                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W571.W571B3.W57108.AUDITA(+1)                        
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.AUDITA(+1)                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W571.W571B3.W57108.AUDITB(+1)                        
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.AUDITB(+1)                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W571.W571B3.W57108.AUDITC(+1)                        
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.AUDITC(+1)                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W571.W571B3.W57108.AUDITD(+1)                        
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.AUDITD(+1)                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY5 EXEC WEMPTST,DSIN=W571.W571B3.W57108.AUDITE(+1)                        
//    IF (EMPTY5.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.AUDITE(+1)                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY6 EXEC WEMPTST,DSIN=W571.W571B3.W57108.EXCLSTA(+1)                       
//    IF (EMPTY6.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.EXCLSTA(+1)                            
//    ENDIF                                                                     
//*                                                                             
//EMPTY7 EXEC WEMPTST,DSIN=W571.W571B3.W57108.EXCLSTB(+1)                       
//    IF (EMPTY7.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.EXCLSTB(+1)                            
//    ENDIF                                                                     
//*                                                                             
//EMPTY8 EXEC WEMPTST,DSIN=W571.W571B3.W57108.EXCLSTC(+1)                       
//    IF (EMPTY8.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.EXCLSTC(+1)                            
//    ENDIF                                                                     
//*                                                                             
//EMPTY9 EXEC WEMPTST,DSIN=W571.W571B3.W57108.EXCLSTD(+1)                       
//    IF (EMPTY9.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.EXCLSTD(+1)                            
//    ENDIF                                                                     
//*                                                                             
//EMPTY10 EXEC WEMPTST,DSIN=W571.W571B3.W57108.EXCLSTE(+1)                      
//    IF (EMPTY10.T.RC = 0) THEN                                                
// EXEC WZ14DAP4,DSIN=W571.W571B3.W57108.EXCLSTE(+1)                            
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W571J008                                         

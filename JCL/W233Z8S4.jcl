//W233Z8S4 JOB (640W2330100W233Z8S4,W100),'RTN W233PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*FILE FOR ALL PLANTS TO S4HANA                                                
//EMPTY  EXEC WEMPTST,DSIN=W233.W233PV.W23346.S4HANA(+0)                        
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W233.W233PV.W23346.S4HANA(+0)                               
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.SPAREPARTVOLUMEFORECAST                                
/*                                                                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233Z8S4                                         

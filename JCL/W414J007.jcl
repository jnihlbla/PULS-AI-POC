//W414J007 JOB (670W4140100W414J007,W100),'RTN W414D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W414    EXEC W414P007                                                         
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=&&W414X7                                            
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*                                                                             
//DAP     EXEC WZ14PDAP,DSIN=&&W414X7                                           
SPX-ONORDER                                                                     
ANYTHING                                                                        
//    ENDIF                                                                     
//*                                                                             
//FREE    EXEC WFREE,NAME=W414D2,MAXRC=8                                        
//SOPEND  EXEC WSOPEND,PROCESS=W414J007                                         

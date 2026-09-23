//W412J023 JOB (670W4120100W412J023,W100),'RTN W412S5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W412    EXEC W412P023                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W412.W412S5.W4122B(+1)                               
//    IF (EMPTY.T.RC = 4) THEN                                                  
//DLET  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W412.W412S5.W4122B(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J023                                         

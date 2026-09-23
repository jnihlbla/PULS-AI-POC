//W460J315 JOB (640W4600100W460J315,W100),'RTN W460S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W460.W460S3.W46012(+0)                              
//*                                                                             
//    IF (EMPTYT.T.RC = 0) THEN                                                 
//W460    EXEC W460P315                                                         
//    ELSE                                                                      
//DEL1  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W460.W460S3.W46012(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//* W46090-FILEN SKAPAS I FÖREGÅENDE W460J305 I DENNA RUTIN                     
//* OCH ANVÄNDS I SENARE I RUTIN W400D1                                         
//* DE FLESTA GENERATIONERNA ÄR TOMMA                                           
//EMPTYT2 EXEC WEMPTST,DSIN=W460.W460S3.W46090(+0)                              
//*                                                                             
//    IF (EMPTYT2.T.RC > 0) THEN                                                
//DEL2  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W460.W460S3.W46090(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460J315                                         

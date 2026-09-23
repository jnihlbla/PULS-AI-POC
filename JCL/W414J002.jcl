//W414J002 JOB (640W4120100W414J002,W100),'RTN W414D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W414    EXEC W414P002                                                         
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W414.W414D1.W41403A                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP".                                                             
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W414.W414D1.W41403A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WZ14DAP3,DSIN=W414.W414D1.W41403A(+1)                              
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W414.W414D1.W41403A(+1),DISP=(OLD,DELETE)                        
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W414J002                                         

//W412J021 JOB (640W4120100W412J021,W100),'RTN W412D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=DESTN                                                    
//      INCLUDE MEMBER=SYST4                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W412    EXEC W412P021                                                         
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W412.W412D8.W41220                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP".                                                             
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W412.W412D8.W41221(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W412.W412D8.W41221(+1)                                    
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W412J021                                         

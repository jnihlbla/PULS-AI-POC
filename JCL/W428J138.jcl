//W428J138 JOB (640W4280100W428J138,W100),'RTN W428D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W428    EXEC W428P138                                                         
//*                                                                             
//***************************************************************               
//*  WZ14DAP3 PROCEDURE WILL PROCESS MULTIPLE FILES FROM THE                    
//*  INPUT FILE W428.W428D3.W42837                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX " ¤DAP" AND FOLLOWED BY:                                            
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPW42838-001                                                  
//*    RCD 2: " ¤DAPXX        , WHERE XX STANDS FOR IDDC                        
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W428.W428D3.W428381(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W428.W428D3.W428381(+1)                                   
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W428J138                                         

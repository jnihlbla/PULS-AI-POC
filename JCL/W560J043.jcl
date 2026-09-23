//W560J043 JOB (650W5600100W560J043,W100),'RTN W560D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W560    EXEC W560P043                                                         
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W560.W560D1.W56043                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPRECLINES"                                                   
//*    RCD 2: " ¤DAPRECLINES" WHERE 41 IS IDDC                                  
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560D1.W56043(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W560.W560D1.W56043(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560J043                                         

//W478J145 JOB (650W2110100W478J145,W100),'RTN W478R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W47845  EXEC W478P045,                                                        
//             DSIN=W478.W478R1.W47843C,                                        
//             INDUT=W478.W478R1                                                
//*                                                                             
//W47845.SYSIN    DD *                                                          
W47845-002                                                                      
//*                                                                             
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W478.W478R1.W47845                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPMAN-DELRFS-P"                                               
//*    RCD 2: " ¤DAP60W47845-002"                                               
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W478.W478R1.W47845(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W478.W478R1.W47845(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W478J145                                         

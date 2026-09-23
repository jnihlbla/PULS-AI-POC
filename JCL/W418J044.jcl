//W418J044 JOB (650W4180100W418J044,W100),'RTN W418D6',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W418     EXEC W418P044                                                        
//*                                                                             
//SORT.SYSIN    DD  DSN=&INDRTE..CONSTANT(W418PD44),DISP=SHR                    
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS A REPORT FROM THE                              
//*  INPUT FILE W418.W418D6.W41844D                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPRECLINES"                                                   
//*    RCD 2: " ¤DAPRECLINES"                                                   
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W418.W418D6.W41844D(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W418.W418D6.W41844D(+1)                                   
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W418J044                                         

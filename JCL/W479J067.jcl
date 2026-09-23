//W479J067 JOB (650W4790100W479J067,W100),'RTN W479R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W47967   EXEC W479P067,                                                       
//             INDIN=W479.W479R1,GEN=,                                          
//             INDUT=W479.W479R1                                                
//*                                                                             
//W47967.SYSIN    DD *                                                          
W47967-001         NOT USED AT ALL. DO NOT REMOVE                               
//*                                                                             
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W479.W479R1.W47968D                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPMAN-INVDC-P                                                 
//*    RCD 2: " ¤DAPXXW47967-001"  WHERE XX IS EU OR CN                         
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W479.W479R1.W47968D(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W479.W479R1.W47968D(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J067                                         

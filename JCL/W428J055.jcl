//W428J055 JOB (640W4280100W428J055,W100),'RTN W428R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W428    EXEC W428P055                                                         
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W428.W428R1.W428551                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX " ¤DAP" AND FOLLOWED BY:                                            
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPW42855-001                                                  
//*    RCD 2: " ¤DAPXX        , WHERE XX = DC                                   
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W428.W428R1.W428551(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W428.W428R1.W428551(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J055                                         

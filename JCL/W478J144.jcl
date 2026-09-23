//W478J144 JOB (650W2110100W478J144,W100),'RTN W478V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W47844  EXEC W478P044,                                                        
//             DSIN=W478.W478V1.W47843,                                         
//             INDUT=W478.W478V1                                                
//*                                                                             
//W47844.SYSIN    DD *                                                          
W47844-002                                                                      
//*                                                                             
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W478.W478V1.W47844                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPDELRFS-W                                                    
//*    RCD 2: " ¤DAP1AW47844-002"  WHERE 01A IS IDDC                            
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W478.W478V1.W47844(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W478.W478V1.W47844(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W478J144                                         

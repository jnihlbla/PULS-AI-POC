//W517J031 JOB (650W5170100W517J031,W100),'RTN W500M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*****************************************************************             
//* MAIL TILL EKONOMI VIA D&P                                                   
//* W517.W500M1.W51730, FORSALJING O BRUTTOVINST                                
//*****************************************************************             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W517.W500M1.W51730(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W517.W500M1.W51730(+0)                                    
//SYSIN           DD *                                                          
W51730-001                                                                      
W51730                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W517J031                                         

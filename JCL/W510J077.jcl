//W510J077 JOB (650W5100100W510J077,W100),'RTN W500M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//* FIL TILL EKOMONI VIA D&P                                                    
//* W510.W500M1.W51077                                                          
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W510.W500M1.W51077(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W510.W500M1.W51077(+0)                                    
//SYSIN           DD *                                                          
W51077-001                                                                      
W51077                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510J077                                         

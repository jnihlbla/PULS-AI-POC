//W522J042 JOB (670W5220100W522J042,W100),'RTN W522M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W522    EXEC W522P042                                                         
//*                                                                             
//******************************************************************            
//* MAIL TILL EKONOMI VIA D&P                                                   
//* DOM VAT SDC+NO W522.W522M1.W52242(+1)                                       
//******************************************************************            
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W522.W522M1.W52242(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W522.W522M1.W52242(+1)                                    
//SYSIN           DD *                                                          
W52242-001                                                                      
W52242                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J042                                         

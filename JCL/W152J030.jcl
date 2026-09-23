//W152J030 JOB (640W1520100W152J030,W100),'RTN W152V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W152    EXEC W152P030                                                         
//*                                                                             
//*                                                                             
//*  -- SKICKA FILEN MED MAIL VIA "DISTRIBUTION AND PRINT"                      
// EXEC WZ14PDAP,DSIN=W152.W152V1.W15230(+1)                                    
//SYSIN           DD *                                                          
W15230-001                                                                      
W1523000                                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152J030                                         

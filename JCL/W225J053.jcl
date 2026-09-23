//W225J053 JOB (670W2250100W225J053,W100),'RTN W225V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//******** COMMON FILE FOR ALL DC'S (41-43,51,61-62,71-73,7A-7H) *******        
//*                                                                             
// EXEC WZ14DAP3,DSIN=W225.W225V3.W22552A(+0)                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225J053                                         

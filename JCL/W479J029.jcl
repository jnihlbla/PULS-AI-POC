//W479J029 JOB (640W4790100W479J029,W100),'RTN W479M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W479    EXEC W479P029                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W479.W479M2.W47929(+1)                                    
//SYSIN           DD *                                                          
W47929-001                                                                      
W47929                                                                          
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W479J029                                         

//W428J134 JOB (640W4280100W428J134,W100),'RTN W428D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W428    EXEC W428P034,                                                        
//             INDIN=W428.W428D3,                                               
//             INDUT=W428.W428D3                                                
//*                                                                             
// EXEC WZ14PDAP,DSIN=W428.W428D3.W42834PR(+1)                                  
//SYSIN           DD *                                                          
W42834-001                                                                      
W42834                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J134                                         

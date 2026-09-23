//W930J002 JOB (670W9300100W930J002,W100),'RTN W930S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W930    EXEC W930P002                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W930.W930S2.W93001(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-1                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-2                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-3                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-4                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-5                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-6                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-7                                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93001(+1)                                    
//SYSIN           DD *                                                          
W93004-001                                                                      
W93004-8                                                                        
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W930.W930S2.W93001(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.MONTHLYCURRENCYRATE                                    
/*                                                                              
//*                                                                             
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W930J002                                         

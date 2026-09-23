//W261V1RS JOB (650W2610100W261V1RS,W100),'RTN W261V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W261V1                                                
//*                                                                             
//*  Om denna rutin skall startas om, av något skäl, så måste                   
//*  man först skapa den infil som renamas här,                                 
//*  eller ALT. hoppa över denna rutinstart ifall den redan körts.              
//*  ...annars                                                                  
//*  Om W483V1RE redan har gått, måste man kopiera filen                        
//*  'W020.V&ÅÅVV.EFRRAD' (där ÅÅVV är föregående vecka)                        
//*  till 'W479.W483V1.EFRRAD(+1)'                                              
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W479.W483V1.EFRRAD',                                           
//           T1='W479.W261V1.EFRRAD',RF1=FB,LR1=167                             
//SOP     EXEC WSOPEND,PROCESS=W261V1RS                                         
